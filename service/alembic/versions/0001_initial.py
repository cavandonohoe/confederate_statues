"""initial schema

Revision ID: 0001_initial
Revises:
Create Date: 2026-06-22

"""

from __future__ import annotations

from collections.abc import Sequence

import sqlalchemy as sa
from alembic import op

revision: str = "0001_initial"
down_revision: str | Sequence[str] | None = None
branch_labels: str | Sequence[str] | None = None
depends_on: str | Sequence[str] | None = None


def upgrade() -> None:
    op.create_table(
        "statues",
        sa.Column("id", sa.String(length=64), primary_key=True),
        sa.Column("source", sa.String(length=64), nullable=False),
        sa.Column("entry", sa.Text(), nullable=False),
        sa.Column("year", sa.Integer(), nullable=False),
        sa.Column(
            "created_at",
            sa.DateTime(timezone=True),
            server_default=sa.func.now(),
            nullable=False,
        ),
        sa.Column(
            "updated_at",
            sa.DateTime(timezone=True),
            server_default=sa.func.now(),
            nullable=False,
        ),
    )
    op.create_index("ix_statues_source", "statues", ["source"])
    op.create_index("ix_statues_year", "statues", ["year"])
    op.create_index("ix_statues_source_year", "statues", ["source", "year"])


def downgrade() -> None:
    op.drop_index("ix_statues_source_year", table_name="statues")
    op.drop_index("ix_statues_year", table_name="statues")
    op.drop_index("ix_statues_source", table_name="statues")
    op.drop_table("statues")
